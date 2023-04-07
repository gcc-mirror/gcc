// https://issues.dlang.org/show_bug.cgi?id=23481

struct flagenum(I = ubyte)
{
	I i = 1;
    alias i this;

	auto opBinary(string s)(int j) {
		return typeof(this)(cast(I)(i*2));
	}
	auto opEquals(I a) {
		return false;
	}
}

enum alphakey
{
    a = flagenum!int(), b, c, d, e, f, g, h, i, j, k, l,
    m, n, o, p, q, r, s, t, u, v, w, x, y, z
}

flagenum!int alpha;

void main()
{
	alpha &= alphakey.a;
    alpha = alpha & alphakey.a; // also crashed in another way
}
