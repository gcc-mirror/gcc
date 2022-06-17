module imports.test23109a;
import imports.test23109c;
import imports.test23109b;
struct Array(T)
{
    T[] data;
    enum SMALLARRAYCAP = 1;
    T[SMALLARRAYCAP] smallarray;
}
alias Ensures = Array!Ensure;
