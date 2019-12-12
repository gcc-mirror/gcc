// REQUIRED_ARGS: -boundscheck=on
// PERMUTE_ARGS: -inline -g -O

import core.exception : RangeError;

// Check for RangeError is thrown
bool thrown(T)(lazy T cond)
{
    import core.exception;
    bool f = false;
    try { cond(); } catch (RangeError e) { f = true; }
    return f;
}

@safe    int safeIndex   (int[] arr) { return arr[2]; }
@trusted int trustedIndex(int[] arr) { return arr[2]; }
@system  int systemIndex (int[] arr) { return arr[2]; }

void main()
{
    int[3] data = [1,2,3];
    int[] arr = data[0..2];

    assert(arr.   safeIndex().thrown);
    assert(arr.trustedIndex().thrown);
    assert(arr. systemIndex().thrown);
}
