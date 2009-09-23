/* { dg-do compile } */
/* { dg-options "-O2 -g" } */

struct __gcov_var {
   unsigned int offset;
   unsigned int buffer[(1 << 10) + 1];
} __gcov_var;
unsigned int * gcov_write_words (unsigned int words) {
   unsigned int *result;
   result = &__gcov_var.buffer[__gcov_var.offset];
   return result;
}

struct gcov_ctr_summary { };
struct gcov_summary {
   unsigned int checksum;
   struct gcov_ctr_summary ctrs[1];
};
void __gcov_write_unsigned (unsigned int);
void __gcov_write_summary (unsigned int tag,
			   const struct gcov_summary *summary)
{
   unsigned ix;
   const struct gcov_ctr_summary *csum;
   __gcov_write_unsigned (summary->checksum);
   for (csum = summary->ctrs, ix = 1; ix--; csum++) { }
}
