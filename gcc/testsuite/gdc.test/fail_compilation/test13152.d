/*
EXTRA_FILES: imports/test13152a.d imports/test13152b.d imports/test13152c.d imports/test13152d.d imports/test13152e.d imports/test13152f.d imports/test13152g.d imports/test13152h.d imports/test13152i.d imports/test13152j.d imports/test13152k.d imports/test13152l.d imports/test13152m.d imports/test13152n.d imports/test13152o.d imports/test13152p.d imports/test13152q.d imports/test13152r.d imports/test13152s.d imports/test13152t.d imports/test13152u.d imports/test13152v.d imports/test13152w.d imports/test13152x.d imports/test13152y.d imports/test13152z.d
TEST_OUTPUT:
---
fail_compilation/test13152.d(12): Error: undefined identifier `x`
---
*/
import imports.test13152a;

void main()
{
    auto y = x;
}
