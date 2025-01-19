/* { dg-do compile } */
/* { dg-options "-ftrapv -mcpu=ev4" } */

typedef struct tnode_t {
        struct tnode_t *tn_left, *tn_right;
        int v_quad;
} tnode_t;

int constant_addr(const tnode_t *, long *);
int constant_addr(const tnode_t *tn, long *offsp)
{
        long offs1 = 0, offs2 = 0;

        if (tn->v_quad > 0) {
                offs1 = tn->v_quad;
                return 0;
        } else if (tn->v_quad > -1) {
                offs2 = tn->tn_right->v_quad;
                if (!constant_addr(tn->tn_left, &offs1))
                        return 0;
        } else {
                return 0;
        }
        *offsp = offs1 + offs2;
        return 1;
}
