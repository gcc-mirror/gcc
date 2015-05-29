/* { dg-options "-O3 -fgraphite-identity -fdump-tree-graphite-all" } */

typedef struct
{
      int mode,state,num,state_out;
      unsigned char* bits;
      char *out;
}test;
unsigned char copy( test* s )
{
   while(1)
     {
      if (s->mode == 0) break;
      if (s->state_out >= s->num) break;
      *(s->out) = s->bits[s->state_out];
      if (s->mode == 0) s->mode++;
     }
}
unsigned char compress(test *in)
{
   unsigned char p_in, p_out;
   while(1)
   {
      if (in->state == 1) 
      {
         p_out |= copy(in);
         if (in->state_out < in->num) break;
      }
   }
   return p_in || p_out;
}

