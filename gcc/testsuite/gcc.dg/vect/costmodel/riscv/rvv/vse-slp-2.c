/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fdump-tree-slp1-details" } */

#define f1 3
#define f2 4
#define f3 5

#define SIZE_X 10
#define SIZE_Y 10
#define SIZE_Z 10

typedef enum {C = 0,
              N, S, E, W, T, B,
              NE, NW, SE, SW,
              NT, NB, ST, SB,
              ET, EB, WT, WB,
              FLAGS, N_CELL_ENTRIES} CELL_ENTRIES;

#define CALC_INDEX(x,y,z,e) ((e)+N_CELL_ENTRIES*((x)+ \
                             (y)*SIZE_X+(z)*SIZE_X*SIZE_Y))
#define GRID_ENTRY_SWEEP(g,dx,dy,dz,e) ((g)[CALC_INDEX(dx, dy, dz, e)+(i)])
#define LOCAL(g,e)       (GRID_ENTRY_SWEEP (g, 0, 0, 0, e))

void foo (unsigned long *grid)
{
    for( int i = CALC_INDEX(0, 0, -2, 0); \
	i < CALC_INDEX(0, 0, SIZE_Z + 2, 0); \
	i += N_CELL_ENTRIES ) {
	LOCAL (grid, C ) = f1;
	LOCAL (grid, N ) = f2;
	LOCAL (grid, S ) = f2;
	LOCAL (grid, E ) = f2;
	LOCAL (grid, W ) = f2;
	LOCAL (grid, T ) = f2;
	LOCAL (grid, B ) = f2;
	LOCAL (grid, NE) = f3;
	LOCAL (grid, NW) = f3;
	LOCAL (grid, SE) = f3;
	LOCAL (grid, SW) = f3;
	LOCAL (grid, NT) = f3;
	LOCAL (grid, NB) = f3;
	LOCAL (grid, ST) = f3;
	LOCAL (grid, SB) = f3;
	LOCAL (grid, ET) = f3;
	LOCAL (grid, EB) = f3;
	LOCAL (grid, WT) = f3;
	LOCAL (grid, WB) = f3;
    }
}

/* { dg-final { scan-tree-dump-times "vectorized using SLP" 0 "slp1" } } */
