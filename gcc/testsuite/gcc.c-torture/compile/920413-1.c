union u {double d;long long ll;};
int f(double x, int n){union u v;v.d=x;if(n>=0){v.ll<<=63;}else{v.ll+=1<<-n;v.ll>>=-n;}return v.ll;}
