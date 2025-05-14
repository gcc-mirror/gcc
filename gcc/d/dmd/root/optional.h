#pragma once

/**
 * Optional implementation.
 *
 * Copyright:   Copyright (C) 1999-2025 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/compiler/src/dmd/root/optional.h, root/_optional.h)
 * Documentation:  https://dlang.org/phobos/dmd_root_optional.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/compiler/src/dmd/root/optional.h
 */

#include "dcompat.h"    // for d_bool

/// Optional type that is either `empty` or contains a value of type `T`
template<typename T>
struct Optional final
{
private:
    /** the value (if present) **/
    T value;

    /** whether `value` is set **/
    d_bool present;

public:
    /** Creates an `Optional` with the given value **/
    Optional(T);

    /** Creates an `Optional` with the given value **/
    static Optional<T> create(T);

    /** Checks whether this `Optional` contains a value **/
    bool isPresent() const;

    /** Checks whether this `Optional` does not contain a value **/
    bool isEmpty() const;

    /** Returns: The value if present **/
    T get();

    bool hasValue(const T) const;
};
