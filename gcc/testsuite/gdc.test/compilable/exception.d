class E2 : Exception { this() { super(null); } }
class E3 : Exception { this() { super(null); } }

void main()
{
    try
    {
    }
    catch (E3)
    {
    }
    catch (E2)
    {
    }
    catch (Exception)
    {
    }
}
