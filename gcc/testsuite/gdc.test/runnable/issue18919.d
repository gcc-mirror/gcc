/*
EXTRA_SOURCES: imports/issue18919b.d
RUN_OUTPUT:
---
imports.issue18919b.func1: issue18919.d:29 issue18919.main void issue18919.main() issue18919
imports.issue18919b.func2: issue18919.d:30 issue18919.main void issue18919.main() issue18919
imports.issue18919b.func3: issue18919.d:31 issue18919.main void issue18919.main() issue18919
imports.issue18919b.func3b: issue18919.d:32 issue18919.main void issue18919.main() issue18919
imports.issue18919b.func3c: issue18919.d:33 issue18919.main void issue18919.main() issue18919
imports.issue18919b.func3d: issue18919.d:34 issue18919.main void issue18919.main() issue18919
imports.issue18919b.func4: issue18919.d:35 issue18919.main void issue18919.main() issue18919
imports.issue18919b.func4b: issue18919.d:36 issue18919.main void issue18919.main() issue18919
imports.issue18919b.func4c: issue18919.d:37 issue18919.main void issue18919.main() issue18919
imports.issue18919b.func4d: issue18919.d:38 issue18919.main void issue18919.main() issue18919
imports.issue18919b.func5!("issue18919.d", 39, "issue18919.main", "void issue18919.main()", "issue18919").func5: issue18919.d:39 issue18919.main void issue18919.main() issue18919
imports.issue18919b.func6!("issue18919.d", 40, "issue18919.main", "void issue18919.main()", "issue18919").func6: issue18919.d:40 issue18919.main void issue18919.main() issue18919
imports.issue18919b.func7: expr1=1082, file=issue18919.d func=issue18919.main, expr3=1
imports.issue18919b.func8: expr1=[42, 1042, ], expr2=[issue18919.d: 42, ], expr3=constant2, expr4=issue18919
imports.issue18919b.func9.fp: issue18919b.d:216 imports.issue18919b
imports.issue18919b.func10: expr1=imports.issue18919b, expr2=imports.issue18919b
imports.issue18919b.func11: issue18919b.d:233   imports.issue18919b
imports.issue18919b.func12: issue18919.d issue18919.main void issue18919.main() issue18919
---
*/
import imports.issue18919b;

void main()
{
    func1();
    func2();
    func3();
    func3b();
    func3c();
    func3d();
    func4();
    func4b();
    func4c();
    func4d();
    func5();
    func6();
    func7();
    func8();
    func9();
    func10();
    func11();
    func12();
}
