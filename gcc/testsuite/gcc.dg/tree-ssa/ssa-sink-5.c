/* { dg-do compile } */
/* { dg-options "-Os -fdump-tree-sink-stats" } */

typedef short int16_t;
typedef unsigned char uint8_t;

void foo(int16_t runs[], uint8_t alpha[], int x, int count)
{
    int16_t* next_runs = runs + x;
    uint8_t*  next_alpha = alpha + x;

    while (x > 0)
    {
        int n = runs[0];

        if (x < n)
        {
            alpha[x] = alpha[0];
            runs[0] = (int16_t)(x);
            runs[x] = (int16_t)(n - x);
            break;
        }
        runs += n;
        alpha += n;
        x -= n;
    }

    runs = next_runs;
    alpha = next_alpha;
    x = count;

   for (;;)
    {
        int n = runs[0];

        if (x < n)
        {
            alpha[x] = alpha[0];
            break;
        }
        x -= n;
        runs += n;
   }
}

/* We should not sink the next_runs = runs + x calculation after the loop.  */
/* { dg-final { scan-tree-dump-times "Sunk statements:" 0 "sink" } } */
/* { dg-final { cleanup-tree-dump "sink" } } */
