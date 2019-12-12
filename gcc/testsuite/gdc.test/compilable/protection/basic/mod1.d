module protection.basic.mod1;

public void publicFoo() {}
package void packageFoo() {}
private void privateFoo() {}

class Test
{
    public void publicFoo();
    protected void protectedFoo();
    package void packageFoo();
    private void privateFoo();
}
