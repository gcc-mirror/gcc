/* { dg-do compile { target powerpc*-*-darwin* } } */
/* { dg-options "-O2 -force_cpusubtype_ALL -mpowerpc64" } */

typedef struct Nlm_rect {
  short sh1;
  short sh2;
  short sh3;
  short sh4;
} S8;

typedef struct udv_mouse_select {
    short Action_type;
    S8 rcClip;
    int pgp;
  } UDVselect;

UDVselect ms;
int UDV(S8 rcClip);

int main()
{
    ms.rcClip.sh1 = 1;
    ms.rcClip.sh4 = 4;
    return UDV(ms.rcClip);
}

int UDV(S8 rcClip){

	return !(rcClip.sh1 == 1 && rcClip.sh4 == 4);
}
	

