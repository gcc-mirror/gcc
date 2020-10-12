// https://bugzilla.gdcproject.org/show_bug.cgi?id=17
// { dg-do run }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }

/**
 * Parameters are not copied into a frame to be accessed from
 * the method's __require function.
 */
void contractTest(string path)
{
    assert(path[0] == 't');
    assert(path.length == 9);
    assert(path[8] == 'i');
}

interface ModuleSaver
{
    void save(string str)
    in
    {
        contractTest(str);
    }
}

class ModuleWriter : ModuleSaver
{
    void save (string str)
    in {}
    do
    {
    }
}

void main()
{
  (new ModuleWriter()).save ("test.0.mci");
}
