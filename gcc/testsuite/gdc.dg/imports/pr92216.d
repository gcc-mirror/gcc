module imports.pr92216;

class B : I
{
    protected override void getStruct(){}
    mixin A!();

}

mixin template A()
{
    public void* getS()
    {
        return null;
    }
}

public interface I
{
    public void* getS();
    protected void getStruct();
}
