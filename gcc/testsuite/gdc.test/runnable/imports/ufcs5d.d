module imports.ufcs5d;

auto f5d1(int)    { return 1; }
auto f5d2(string) { return 2; }
auto f5d3(double) { return 3; }
alias f5d4 = f5d1, f5d4 = f5d2;
alias f5d5 = f5d3, f5d5 = f5d4;

@property p5d1(int)    { return 1; }    @property p5d1(int,    int) { return 1; }
@property p5d2(string) { return 2; }    @property p5d2(string, int) { return 2; }
@property p5d3(double) { return 3; }    @property p5d3(double, int) { return 3; }
alias p5d4 = p5d1, p5d4 = p5d2;         alias p5d4 = p5d1, p5d4 = p5d2;
alias p5d5 = p5d3, p5d5 = p5d4;         alias p5d5 = p5d3, p5d5 = p5d4;
