module imports.std11069container;

import imports.std11069range, imports.std11069typecons;

struct BinaryHeap(Store)
if (isInputRange!Store)
//if (isRandomAccessRange!Store)
{
    // The payload includes the support store and the effective length
    private RefCounted!(Tuple!(Store)) _payload;
}
