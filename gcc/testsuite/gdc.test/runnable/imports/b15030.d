module b;

import imports.std15030algo;

unittest
{
    auto dg =
        // __lambda1
        (int[] delegate(int[]) self) =>
            // __lambda2
            (int[] arr) => arr
                           ? self([arr.filter!(
                                // __lambda2.__lambda2
                                a => a
                             ).front])
                           : null;
}
