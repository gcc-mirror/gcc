template<typename T>
struct X {
    double & f (const unsigned int i,
                 const unsigned int j);
    void g (X &M);
};

template <typename T>
void X<T>::g (X &x) {
        double t14 = x.f(0,0)*x.f(1,1);
        double t15 = x.f(2,2)*x.f(3,3);
        double t17 = x.f(2,3)*x.f(3,2);
        double t19 = x.f(0,0)*x.f(2,1);
        double t20 = x.f(1,2)*x.f(3,3);
        double t22 = x.f(1,3)*x.f(3,2);
        double t24 = x.f(0,0)*x.f(3,1);
        double t25 = x.f(1,2)*x.f(2,3);
        double t27 = x.f(1,3)*x.f(2,2);
        double t29 = x.f(1,0)*x.f(0,1);
        double t32 = x.f(1,0)*x.f(2,1);
        double t33 = x.f(0,2)*x.f(3,3);
        double t35 = x.f(0,3)*x.f(3,2);
        double t37 = x.f(1,0)*x.f(3,1);
        double t38 = x.f(0,2)*x.f(2,3);
        double t40 = x.f(0,3)*x.f(2,2);
        double t42 = t14*t15-t14*t17-t19*t20+t19*t22+
                           t24*t25-t24*t27-t29*t15+t29*t17+
                           t32*t33-t32*t35-t37*t38+t37*t40;
        double t43 = x.f(2,0)*x.f(0,1);
        double t46 = x.f(2,0)*x.f(1,1);
        double t49 = x.f(2,0)*x.f(3,1);
        double t50 = x.f(0,2)*x.f(1,3);
        double t52 = x.f(0,3)*x.f(1,2);
        double t54 = x.f(3,0)*x.f(0,1);
        double t57 = x.f(3,0)*x.f(1,1);
        double t60 = x.f(3,0)*x.f(2,1);
        double t63 = t43*t20-t43*t22-t46*t33+t46*t35+
                           t49*t50-t49*t52-t54*t25+t54*t27+
                           t57*t38-t57*t40-t60*t50+t60*t52;
        double t65 = 1/(t42+t63);
        double t71 = x.f(0,2)*x.f(2,1);
        double t73 = x.f(0,3)*x.f(2,1);
        double t75 = x.f(0,2)*x.f(3,1);
        double t77 = x.f(0,3)*x.f(3,1);
        double t81 = x.f(0,1)*x.f(1,2);
        double t83 = x.f(0,1)*x.f(1,3);
        double t85 = x.f(0,2)*x.f(1,1);
        double t87 = x.f(0,3)*x.f(1,1);
        double t101 = x.f(1,0)*x.f(2,2);
        double t103 = x.f(1,0)*x.f(2,3);
        double t105 = x.f(2,0)*x.f(1,2);
        double t107 = x.f(2,0)*x.f(1,3);
        double t109 = x.f(3,0)*x.f(1,2);
        double t111 = x.f(3,0)*x.f(1,3);
        double t115 = x.f(0,0)*x.f(2,2);
        double t117 = x.f(0,0)*x.f(2,3);
        double t119 = x.f(2,0)*x.f(0,2);
        double t121 = x.f(2,0)*x.f(0,3);
}

template void X<double>::g (X<double>&);
