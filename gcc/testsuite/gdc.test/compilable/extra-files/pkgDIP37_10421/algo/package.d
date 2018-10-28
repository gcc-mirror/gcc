module pkgDIP37_10421.algo;
public import pkgDIP37_10421.algo.mod;
package
{
    void foo() {}
    void bar() { foo(); }   // should be accessible
}
