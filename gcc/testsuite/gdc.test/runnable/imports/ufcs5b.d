module imports.ufcs5b;

auto f5b1(int)    { return 1; }
auto f5b2(string) { return 2; }
auto f5b3(double) { return 3; }
alias f5b4 = f5b1, f5b4 = f5b2;
alias f5b5 = f5b3, f5b5 = f5b4;

@property p5b1(int)    { return 1; }    @property p5b1(int,    int) { return 1; }
@property p5b2(string) { return 2; }    @property p5b2(string, int) { return 2; }
@property p5b3(double) { return 3; }    @property p5b3(double, int) { return 3; }
alias p5b4 = p5b1, p5b4 = p5b2;         alias p5b4 = p5b1, p5b4 = p5b2;
alias p5b5 = p5b3, p5b5 = p5b4;         alias p5b5 = p5b3, p5b5 = p5b4;

/***************************************/

auto      f5ov(int)      { return 1; }
@property p5ov(int)      { return 1; }
@property p5ov(int, int) { return 1; }
