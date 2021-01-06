module line;

static assert(__LINE__ == 3);

int #line 10
x;

static assert(__LINE__ == 12);
version(Windows) {
    static assert(__FILE__ == "compilable\\line.d");
    static assert(__FILE_FULL_PATH__[1..3] == ":\\");
} else {
    static assert(__FILE__ == "compilable/line.d");
    static assert(__FILE_FULL_PATH__[0] == '/');
}
static assert(__FILE_FULL_PATH__[$-__FILE__.length..$] == __FILE__);

#line 100 "newfile.d"

static assert(__LINE__ == 101);
static assert(__FILE__ == "newfile.d");
static assert(__FILE_FULL_PATH__[$ - 9 .. $] == "newfile.d");

# line 200

static assert(__LINE__ == 201);
static assert(__FILE__ == "newfile.d");
static assert(__FILE_FULL_PATH__[$ - 9 .. $] == "newfile.d");


