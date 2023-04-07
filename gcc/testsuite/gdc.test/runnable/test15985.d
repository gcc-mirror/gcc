/* PERMUTE_ARGS: -allinst
 */

// https://issues.dlang.org/show_bug.cgi?id=15985

void ff()()
{
    gg!()();
    hh!()();
}

void gg()() { ff!()(); }
void hh()() { ff!()(); }

enum x = is(typeof(ff!()()));
alias my_g = gg!();

int main() { return 0; }
