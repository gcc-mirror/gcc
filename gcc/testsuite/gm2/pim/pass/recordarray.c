
typedef struct srecord {
  int x;
  int y;
  int z;
} record ;



static record a[50];

main()
{
  int i = 1;

  a[i].x = 1;
  a[i].y = 2;
  a[i].z = 3;
}
