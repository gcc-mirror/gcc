// https://issues.dlang.org/show_bug.cgi?id=22813
struct Template(int i) { }
uint test22813()
{
    Template!(1) x;
    return 0;
}
immutable constant = test22813();
alias X = Template!constant;
