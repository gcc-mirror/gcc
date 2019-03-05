module imports.ufcs5c;

auto f5c1(int)    { return 1; }
auto f5c2(string) { return 2; }
auto f5c3(double) { return 3; }
alias f5c4 = f5c1, f5c4 = f5c2;
alias f5c5 = f5c3, f5c5 = f5c4;

@property p5c1(int)    { return 1; }    @property p5c1(int,    int) { return 1; }
@property p5c2(string) { return 2; }    @property p5c2(string, int) { return 2; }
@property p5c3(double) { return 3; }    @property p5c3(double, int) { return 3; }
alias p5c4 = p5c1, p5c4 = p5c2;         alias p5c4 = p5c1, p5c4 = p5c2;
alias p5c5 = p5c3, p5c5 = p5c4;         alias p5c5 = p5c3, p5c5 = p5c4;

/***************************************/

auto      f5ov(string)      { return 2; }
@property p5ov(string)      { return 2; }
@property p5ov(string, int) { return 2; }
