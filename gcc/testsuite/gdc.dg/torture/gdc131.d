// https://bugzilla.gdcproject.org/show_bug.cgi?id=131
// { dg-do run }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }

struct S131
{
    this(string ) { }
    string opAssign(string v) { return v; }
}

void main()
{
    S131[string] s;
    s["foo"] = "bar";
}
