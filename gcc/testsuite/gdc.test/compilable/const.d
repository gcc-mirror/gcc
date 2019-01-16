
static assert(2.0  * 3.0  ==  6 );
static assert(2.0  * 3.0i ==  6i);
static assert(2.0i * 3.0  ==  6i);
static assert(2.0i * 3.0i == -6 );

static assert(2.0  * (4.0 + 3.0i) ==  8  + 6i);
static assert(2.0i * (4.0 + 3.0i) ==  8i - 6 );
static assert((4.0 + 3.0i) * 2.0  ==  8  + 6i);
static assert((4.0 + 3.0i) * 2.0i ==  8i - 6 );
static assert((4.0 + 3.0i) * (5 + 7i) ==  -1 + 43i );

static assert((2.0).re == 2);
static assert((2.0i).re == 0);
static assert((3+2.0i).re == 3);

static assert((4.0i).im == 4);
static assert((2.0i).im == 2);
static assert((3+2.0i).im == 2);

static assert(6.0 / 2.0 == 3);
static assert(6i / 2i ==  3);
static assert(6  / 2i == -3i);
static assert(6i / 2  ==  3i);

static assert((6 + 4i) / 2 == 3 + 2i);
static assert((6 + 4i) / 2i == -3i + 2);

//static assert(2 / (6 + 4i) == -3i);
//static assert(2i / (6 + 4i)  ==  3i);
//static assert((1 + 2i) / (6 + 4i)  ==  3i);

static assert(6.0 % 2.0 == 0);
static assert(6.0 % 3.0 == 0);
static assert(6.0 % 4.0 == 2);

static assert(6.0i % 2.0i == 0);
static assert(6.0i % 3.0i == 0);
static assert(6.0i % 4.0i == 2i);


