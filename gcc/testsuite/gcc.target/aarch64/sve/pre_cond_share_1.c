/* { dg-do compile } */
/* { dg-options "-Ofast -fdump-tree-optimized" } */

#include <stdint.h>
#include <stddef.h>
#include <math.h>
#include <float.h>

typedef struct __attribute__((__packed__)) _Atom {
    float x, y, z;
    int32_t type;
} Atom;

typedef struct __attribute__((__packed__)) _FFParams {
    int32_t hbtype;
    float radius;
    float hphb;
    float elsc;
} FFParams;

#ifndef PPWI
#define PPWI (64)
#endif

#ifndef ITERS
#define ITERS 8
#endif

#define DIFF_TOLERANCE_PCT 0.025f

#define POSES_SIZE 393216
#define PROTEIN_SIZE 938
#define LIGAND_SIZE 26
#define FORCEFIELD_SIZE 34

#define ZERO 0.0f
#define QUARTER 0.25f
#define HALF 0.5f
#define ONE 1.0f
#define TWO 2.0f
#define FOUR 4.0f
#define CNSTNT 45.0f

// Energy evaluation parameters
#define HBTYPE_F 70
#define HBTYPE_E 69
#define HARDNESS 38.0f
#define NPNPDIST 5.5f
#define NPPDIST 1.0f

void
fasten_main(size_t group, size_t ntypes, size_t nposes, size_t natlig, size_t natpro,        //
            const Atom *protein, const Atom *ligand,                                         //
            const float *transforms_0, const float *transforms_1, const float *transforms_2, //
            const float *transforms_3, const float *transforms_4, const float *transforms_5, //
            const FFParams *forcefield, float *energies                                      //
) {

    float etot[PPWI];
    float lpos_x[PPWI];

    for (int l = 0; l < PPWI; l++) {
        etot[l] = 0.f;
        lpos_x[l] = 0.f;
    }

    // Loop over ligand atoms
    for (int il = 0; il < natlig; il++) {
        // Load ligand atom data
        const Atom l_atom = ligand[il];
        const FFParams l_params = forcefield[l_atom.type];
        const int lhphb_ltz = l_params.hphb < 0.f;
        const int lhphb_gtz = l_params.hphb > 0.f;

        // Transform ligand atom

        // Loop over protein atoms
        for (int ip = 0; ip < natpro; ip++) {
            // Load protein atom data
            const Atom p_atom = protein[ip];
            const FFParams p_params = forcefield[p_atom.type];

            const float radij = p_params.radius + l_params.radius;
            const float r_radij = ONE / radij;

            const float elcdst = (p_params.hbtype == HBTYPE_F && l_params.hbtype == HBTYPE_F) ? FOUR
                                                                                              : TWO;
            const float elcdst1 = (p_params.hbtype == HBTYPE_F && l_params.hbtype == HBTYPE_F)
                                  ? QUARTER : HALF;
            const int type_E = ((p_params.hbtype == HBTYPE_E || l_params.hbtype == HBTYPE_E));

            const int phphb_ltz = p_params.hphb < 0.f;
            const int phphb_gtz = p_params.hphb > 0.f;
            const int phphb_nz = p_params.hphb != 0.f;
            const float p_hphb = p_params.hphb * (phphb_ltz && lhphb_gtz ? -ONE : ONE);
            const float l_hphb = l_params.hphb * (phphb_gtz && lhphb_ltz ? -ONE : ONE);
            const float distdslv = (phphb_ltz ? (lhphb_ltz ? NPNPDIST : NPPDIST) : (lhphb_ltz
                                                                                    ? NPPDIST
                                                                                    : -FLT_MAX));
            const float r_distdslv = ONE / distdslv;

            const float chrg_init = l_params.elsc * p_params.elsc;
            const float dslv_init = p_hphb + l_hphb;

            for (int l = 0; l < PPWI; l++) {
                // Calculate distance between atoms
                const float x = lpos_x[l] - p_atom.x;
                const float distij = (x * x);

                // Calculate the sum of the sphere radii
                const float distbb = distij - radij;

                const int zone1 = (distbb < ZERO);

                // Calculate formal and dipole charge interactions
                float chrg_e = chrg_init * ((zone1 ? ONE : (ONE - distbb * elcdst1)) *
                                            (distbb < elcdst ? ONE : ZERO));
                float neg_chrg_e = -fabsf(chrg_e);
                chrg_e = type_E ? neg_chrg_e : chrg_e;
                etot[l] += chrg_e * CNSTNT;
            }
        }
    }

    // Write result
    for (int l = 0; l < PPWI; l++) {
        energies[group * PPWI + l] = etot[l] * HALF;
    }
}

/* { dg-final { scan-tree-dump-times {\.COND_MUL} 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times {\.VCOND} 1 "optimized" { xfail *-*-* } } } */
