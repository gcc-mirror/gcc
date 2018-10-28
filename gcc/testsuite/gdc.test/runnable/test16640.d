// PERMUTE_ARGS:

void testFileFullPathAsDefaultArgument(string preBakedFileFullPath, string fileFullPath = __FILE_FULL_PATH__)
{
    assert(preBakedFileFullPath == fileFullPath);
}

void main()
{
    testFileFullPathAsDefaultArgument(__FILE_FULL_PATH__);
}
