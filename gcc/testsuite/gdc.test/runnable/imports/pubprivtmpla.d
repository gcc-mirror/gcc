module pubprivtmpla;

struct S
{
   private int m = 42;
   private int _get()() { return m; }
   public alias get = _get!();
}
