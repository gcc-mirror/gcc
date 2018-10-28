// PERMUTE_ARGS:
struct S
{
        const char* path;
        @disable this();
        this(const(char)* path)
        {
                this.path = path;
        }
}
const S CONST_S = S("/tmp".ptr);

