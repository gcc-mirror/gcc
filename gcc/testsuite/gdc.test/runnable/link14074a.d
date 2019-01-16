// EXTRA_SOURCES: imports/link14074y.d
import imports.link14074x;
import imports.link14074y;

struct Inner
{
}

struct Test
{
    Inner inner;
}


void main()
{
    ubyte[] buffer;
    Test test;

    encodeArray(buffer, test);
}
