/* { dg-do compile } */
/* { dg-require-effective-target vect_float } */

struct S {
    float m_col1[4];
    float m_col2[4];
    float m_col3[4];
    float m_col4[4];
};

void apply(struct S *s, const float *in, float *out, long numPixels)
{
  for (long idx = 0; idx < numPixels; ++idx)
    {
      const float r = in[0];
      const float g = in[1];
      const float b = in[2];
      const float a = in[3];
      out[0] = r*s->m_col1[0] + g*s->m_col2[0] + b*s->m_col3[0] + a*s->m_col4[0];
      out[1] = r*s->m_col1[1] + g*s->m_col2[1] + b*s->m_col3[1] + a*s->m_col4[1];
      out[2] = r*s->m_col1[2] + g*s->m_col2[2] + b*s->m_col3[2] + a*s->m_col4[2];
      out[3] = r*s->m_col1[3] + g*s->m_col2[3] + b*s->m_col3[3] + a*s->m_col4[3];
      in  += 4;
      out += 4;
    }
}

/* { dg-final { scan-tree-dump "vectorization factor = 1" "vect" { target { ! vect_load_lanes } } } } */
