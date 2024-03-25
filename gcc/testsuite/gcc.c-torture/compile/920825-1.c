#pragma pack(1)
struct{unsigned short f1:5;unsigned short f2:6;}x;
void f(void){x.f2=1;}
