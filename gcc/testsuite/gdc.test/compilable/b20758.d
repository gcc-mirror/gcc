module b20758;

template foo(A...) { }

int attr() {return 1;}
@attr int y;

alias A = __traits(getAttributes, y);
alias B = __traits(getOverloads, b20758, "attr");
static assert(__traits(isSame, foo!(A[0]), foo!(attr)));
static assert(__traits(isSame, foo!(A), foo!(attr)));
static assert(__traits(isSame, foo!(attr), foo!(B[0])));
static assert(__traits(isSame, foo!(attr), foo!(B)));

void main() { }
