/* { dg-lto-do link } */
/* { dg-lto-options {{-fPIC -r -nostdlib -flto}} } */

typedef struct {
    int NumPackStreams;
} CSzAr;
typedef struct {
    CSzAr db;
} CSzArEx;
int SzArEx_Init(CSzArEx *p)
{
  return p->db.NumPackStreams;
}
int SzArEx_GetFolderFullPackSize(const CSzArEx *p)
{
  return p->db.NumPackStreams;
}

