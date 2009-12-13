// { dg-do compile }
typedef unsigned char uint8;
typedef unsigned int uint32;
class PixelARGB {
public:
    ~PixelARGB() throw() { }
    PixelARGB (const uint32 argb_) throw() : argb (argb_)     { }
    inline __attribute__((always_inline)) uint8 getRed() const throw() {
	return components.r;
    }
    union     {
	uint32 argb;
	struct         {
	    uint8 b, g, r, a;
	} components;
    };
};
class Colour {
public:
    Colour() throw() : argb (0) {};
    uint8 getRed() const throw() {
	return argb.getRed();
    }
    PixelARGB argb;
};
uint8 writeImage (void) {
    Colour pixel;
    pixel = Colour ();
    return pixel.getRed();
}
