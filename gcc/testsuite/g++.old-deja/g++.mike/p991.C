// Build don't link:
// prms-id: 991

class Complex {
public:
	double re;
	double im;
	Complex(double r,double i) : re(r), im(i) {}
};

Complex cos(const Complex&);

extern "C" double cos (double);		// not the same as the above
extern "C" double cosh (double);
extern "C" double sin (double);
extern "C" double sinh (double);

Complex cos(const Complex& arg) {
	double nr = cos(arg.re)*cosh(arg.im);
	double ni = -sin(arg.re)*sinh(arg.im);
	return Complex(nr,ni);
}
