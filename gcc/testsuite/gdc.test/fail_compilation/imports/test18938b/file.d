module imports.test18938b.file;
import std.algorithm;

class IniLikeGroup
{
    this(string ) {}
    @trusted byNode()
    {
        map!(node => lineNode);
    }
}


class IniLikeFile
{
    struct WriteOptions
    {
        static exact()
        {
            return WriteOptions(No.lineBetweenGroups);
        }

        this(Args)(Args ){}

    }
    void saveToFile(WriteOptions = WriteOptions.exact) {}
    final save() {}
}
