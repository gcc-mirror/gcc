typedef unsigned char qi;
typedef unsigned short hi;
typedef unsigned long si;
typedef unsigned long long di;
subi(a){return 100-a;}
add(a,b){return a+b;}
mul(a){return 85*a;}
memshift(p)unsigned*p;{unsigned x;for(;;){x=*p++>>16;if(x)return x;}}
ldw(xp)si*xp;{return xp[4];}
ldws_m(xp)si*xp;{si x;do{x=xp[3];xp+=3;}while(x);}
postinc_si(p)si*p;{si x;for(;;){x=*p++;if(x)return x;}}
preinc_si(p)si*p;{si x;for(;;){x=*++p;if(x)return x;}}
postinc_di(p)di*p;{di x;for(;;){x=*p++;if(x)return x;}}
preinc_di(p)di*p;{di x;for(;;){x=*++p;if(x)return x;}}
inc_overlap(p,a)di*p;{do{p=*(di**)p;p=(di*)((int)p+4);}while(*p);}
di move_di(p,p2)di*p,*p2;{di x=p;p2=((di*)x)[1];return p2[1];}
