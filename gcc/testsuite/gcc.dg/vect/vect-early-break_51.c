/* { dg-add-options vect_early_break } */
/* { dg-do compile } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */

int x_in[32];
int x_out_a[32], x_out_b[32];
int c[16] = {3,2,1,10,1,42,3,4,50,9,32,8,11,10,1,2};
int a[16 +1] = {0,16,32,48,64,128,256,512,0,16,32,48,64,128,256,512,1024};
int b[16 +1] = {17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1};

void foo ()
{
  int j, i, x;
  int curr_a, flag, next_a, curr_b, next_b;
    {
      for (i = 0; i < 16; i++)
        {
          next_b = b[i+1];
          curr_b = flag ? next_b : curr_b;
        }
      x_out_b[j] = curr_b;
    }
}

/* { dg-final { scan-tree-dump "vectorized 1 loops in function" "vect" } } */