/* { dg-do compile } */

int main_i;
void transparent_crc(int);
#pragma pack(1)
struct {
  signed : 17;
  signed : 6;
  unsigned : 13;
  unsigned f6 : 12;
} g_20[1];
int main()
{
  transparent_crc(g_20[main_i].f6);
  return 0;
}
