import imports.link14541traits;

void main()
{
    Tuple!(int, int) result;

    alias T = typeof(result);
    static assert(hasElaborateAssign!T);
    // hasElaborateAssign!(Tuple(int, int)):
    // 1. instantiates Tuple!(int, int).opAssign!(Tuple!(int, int)) [auto ref = Rvalue]
    //    2. instantiates swap!(Tuple!(int, int))
    //       3. instantiates hasElaborateAssign!(Tuple!(int, int))
    //          --> forward reference error
    //       --> swap!(Tuple!(int, int)) fails to instantiate
    //    --> Tuple!(int, int).opAssign!(Tuple!(int, int)) [auto ref = rvalue] fails to instantiate
    // 4. instantiates Tuple!(int, int).opAssign!(Tuple!(int, int)) [auto ref = Lvalue]
    //    --> succeeds
    // hasElaborateAssign!(Tuple(int, int)) succeeds to instantiate (result is 'true')

    // Instantiates Tuple!(int, int).opAssign!(Tuple!(int, int)) [auto ref = Rvalue], but
    // it's already done in gagged context, so this is made an error reproduction instantiation.
    // But, the forward reference of hasElaborateAssign!(Tuple(int, int)) is already resolved, so
    // the instantiation will succeeds.
    result = Tuple!(int, int)(0, 0);    // --> 1st error reproduction instantiation
    result = Tuple!(int, int)(0, 0);    // --> 2nd error reproduction instantiation

    // The two error reproduction instantiations generate the function:
    //   Tuple!(int, int).opAssign!(Tuple!(int, int)) [auto ref = Rvalue]
    // twice, then it will cause duplicate COMDAT error in Win64 platform.
}

/+
The point is, if instantiated contexts are different, two instantiations may cause different result.

- The 1st Tuple.opAssign instantiation is invoked from hasElaborateAssign template with gagging.
  So it has failed, because of the circular reference of hasElaborateAssign template..

- The 2nd Tuple.opAssign instantiation is invoked from main() without gagging.
  It does not have circular reference, so the instantiation should succeed.

Therefore, the gagged failure should be overridden by the ungagged success.
+/
