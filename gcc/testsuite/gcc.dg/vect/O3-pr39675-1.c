/* { dg-do compile } */

typedef struct {
    float vertexAttrib[16][4];
    float vertexAttribPre[16][4];
    float rasterAttrib[16][4];
    float rasterAttribPre[16][4];
} CRCurrentState;
void crStateCurrentInit(CRCurrentState *c)
{
  unsigned int i;
  for (i = 0; i < 16; i++)
    {
      (c->vertexAttribPre[i])[0] = (c->vertexAttrib[i])[0];
      (c->vertexAttribPre[i])[1] = (c->vertexAttrib[i])[1];
      (c->vertexAttribPre[i])[2] = (c->vertexAttrib[i])[2];
      (c->vertexAttribPre[i])[3] = (c->vertexAttrib[i])[3];
      (c->rasterAttrib[i])[0] = (c->vertexAttrib[i])[0];
      (c->rasterAttrib[i])[1] = (c->vertexAttrib[i])[1];
      (c->rasterAttrib[i])[2] = (c->vertexAttrib[i])[2];
      (c->rasterAttrib[i])[3] = (c->vertexAttrib[i])[3];
      (c->rasterAttribPre[i])[0] = (c->vertexAttrib[i])[0];
      (c->rasterAttribPre[i])[1] = (c->vertexAttrib[i])[1];
      (c->rasterAttribPre[i])[2] = (c->vertexAttrib[i])[2];
      (c->rasterAttribPre[i])[3] = (c->vertexAttrib[i])[3];
    }
}

/* { dg-final { cleanup-tree-dump "vect" } } */

