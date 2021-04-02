module test20280a;

struct Alpha(uint _)
{
    import test20280a;
}

struct Foxtrot(uint _)
{
    alias Attributes = Alpha!10;
    enum A = 10;
}
