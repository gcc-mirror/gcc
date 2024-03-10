
enum Test;

@true @null @byte int x;
@(int) int y;
@"test" @`test2` @30 @'a' @__LINE__ void f();

@Test void h();

static assert(   __traits(getAttributes, x)[0] == true);
static assert(   __traits(getAttributes, x)[1] == null);
static assert(is(__traits(getAttributes, x)[2] == byte));

static assert(is(__traits(getAttributes, y)[0] == int));

static assert(   __traits(getAttributes, f)[0] == "test");
static assert(   __traits(getAttributes, f)[1] == "test2");
static assert(   __traits(getAttributes, f)[2] == 30);
static assert(   __traits(getAttributes, f)[3] == 'a');
static assert(   __traits(getAttributes, f)[4] == 6);

static assert(is(__traits(getAttributes, h)[0] == enum));
