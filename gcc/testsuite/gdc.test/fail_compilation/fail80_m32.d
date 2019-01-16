// REQUIRED_ARGS: -m32
/*
TEST_OUTPUT:
---
fail_compilation/fail80_m32.d(28): Error: cannot implicitly convert expression `"progress_rem"` of type `string` to `uint`
fail_compilation/fail80_m32.d(29): Error: cannot implicitly convert expression `"redo"` of type `string` to `uint`
---
*/

module paintshop;

class Image{}

class ResourceManager
{
    Image getImage(char[] name) { return null; }
}

class Test
{
    import std.file;
    import std.path;

    static Image[] images;

    static void initIcons() 
    {
        images["progress_rem"]  = ResourceManager.getImage("progress_rem.gif"); // delete_obj_dis
        images["redo"]          = ResourceManager.getImage("redo.gif");
    }
}
