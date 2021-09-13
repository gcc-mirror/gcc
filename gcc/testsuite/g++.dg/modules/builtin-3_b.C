// { dg-additional-options -fmodules-ts }
import builtins;

int main ()
{
  length ("");
  count (1, "", "", nullptr);
}
