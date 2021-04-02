module test18322import;
void fun(string templateFileFullPath = __FILE_FULL_PATH__,
    string templateFile = __FILE__)(string expectedFilename, string fileFullPath = __FILE_FULL_PATH__)
{
    // make sure it is an absolute path
    version(Windows)
        assert(fileFullPath[1..3] == ":\\");
    else
        assert(fileFullPath[0] == '/');

    assert(templateFileFullPath == fileFullPath);
    assert(fileFullPath[$ - expectedFilename.length .. $] == expectedFilename);
    assert(fileFullPath[$ - templateFile.length .. $] == templateFile);
}
