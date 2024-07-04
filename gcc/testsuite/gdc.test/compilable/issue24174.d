/* TEST_OUTPUT:
---
true
false
---
*/

bool func1() {
    struct BtMatcher {
        uint pc = 0;
    }
    BtMatcher matcher;
    with (matcher) {
        goto StartLoop;
        StartLoop:
        goto SecondLabel;
        SecondLabel:
        return true;
    }
}

bool func2() {
    try {
        throw new Exception("a");
        return true;
    } catch (Exception e) {
        goto StartA;
        StartA:
        goto StartB;
        StartB:
        return false;
    }
}

pragma(msg, func1());
pragma(msg, func2());
