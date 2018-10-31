module imports.ufcs5a;

auto f5a1(int)    { return 1; }
auto f5a2(string) { return 2; }
auto f5a3(double) { return 3; }
alias f5a4 = f5a1, f5a4 = f5a2;
alias f5a5 = f5a3, f5a5 = f5a4;

@property p5a1(int)    { return 1; }    @property p5a1(int,    int) { return 1; }
@property p5a2(string) { return 2; }    @property p5a2(string, int) { return 2; }
@property p5a3(double) { return 3; }    @property p5a3(double, int) { return 3; }
alias p5a4 = p5a1, p5a4 = p5a2;         alias p5a4 = p5a1, p5a4 = p5a2;
alias p5a5 = p5a3, p5a5 = p5a4;         alias p5a5 = p5a3, p5a5 = p5a4;
