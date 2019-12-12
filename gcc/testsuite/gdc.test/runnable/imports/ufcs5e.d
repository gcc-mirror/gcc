module imports.ufcs5e;

auto f5e1(int)    { return 1; }
auto f5e2(string) { return 2; }
auto f5e3(double) { return 3; }
alias f5e4 = f5e1, f5e4 = f5e2;
alias f5e5 = f5e3, f5e5 = f5e4;

@property p5e1(int)    { return 1; }    @property p5e1(int,    int) { return 1; }
@property p5e2(string) { return 2; }    @property p5e2(string, int) { return 2; }
@property p5e3(double) { return 3; }    @property p5e3(double, int) { return 3; }
alias p5e4 = p5e1, p5e4 = p5e2;         alias p5e4 = p5e1, p5e4 = p5e2;
alias p5e5 = p5e3, p5e5 = p5e4;         alias p5e5 = p5e3, p5e5 = p5e4;
