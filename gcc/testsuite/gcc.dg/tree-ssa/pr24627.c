/* { dg-do run } */
/* { dg-options "-O" } */
/* { dg-require-effective-target int32plus } */

extern void abort (void);
typedef unsigned int u_int32;

typedef struct {
  union {u_int32 Xl_ui;} Ul_i;
  union {u_int32 Xl_uf;} Ul_f;
} l_fp;

void
dolfptoa (short ndec)
{
  l_fp work, ftmp;

  work.Ul_i.Xl_ui = 0;
  work.Ul_f.Xl_uf = 0x535f3d8;

  while (ndec > 0)
    {
      u_int32 lo_tmp;
      u_int32 hi_tmp;

      ndec--;
      work.Ul_i.Xl_ui = 0;
      work.Ul_i.Xl_ui <<= 1;
      if ((work.Ul_f.Xl_uf) & 0x80000000)
	(work.Ul_i.Xl_ui) |= 0x1;
      (work.Ul_f.Xl_uf) <<= 1;

      ftmp = work;
      (work.Ul_i.Xl_ui) <<= 1;
      if ((work.Ul_f.Xl_uf) & 0x80000000)
	(work.Ul_i.Xl_ui) |= 0x1;
      (work.Ul_f.Xl_uf) <<= 1;

      (work.Ul_i.Xl_ui) <<= 1;
      if ((work.Ul_f.Xl_uf) & 0x80000000)
	(work.Ul_i.Xl_ui) |= 0x1;
      (work.Ul_f.Xl_uf) <<= 1;

      lo_tmp = ((work.Ul_f.Xl_uf) & 0xffff) + ((ftmp.Ul_f.Xl_uf) & 0xffff);
      hi_tmp = (((work.Ul_f.Xl_uf) >> 16) & 0xffff)
               + (((ftmp.Ul_f.Xl_uf) >> 16) & 0xffff);

      if (lo_tmp & 0x10000)
	hi_tmp++;

      (work.Ul_f.Xl_uf) = ((hi_tmp & 0xffff) << 16) | (lo_tmp & 0xffff);
      (work.Ul_i.Xl_ui) += (ftmp.Ul_i.Xl_ui);

      if (hi_tmp & 0x10000)
	(work.Ul_i.Xl_ui)++;


      if (!(work.Ul_i.Xl_ui < 10))
	abort ();
    }
}

int main()
{
  dolfptoa(6);
  return 0;
}
