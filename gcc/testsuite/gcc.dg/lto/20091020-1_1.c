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

