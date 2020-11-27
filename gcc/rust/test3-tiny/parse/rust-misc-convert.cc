#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "fold-const.h"
#include "convert.h"
// order: config, system, coretypes, tree, fold-const, convert

// Required by some generic routines. Converts expression of value expr to type type.
tree convert(tree type, tree expr) {
    if (type == error_mark_node || expr == error_mark_node || TREE_TYPE(expr) == error_mark_node)
        return error_mark_node;

    if (type == TREE_TYPE(expr))
        return expr;

    if (TYPE_MAIN_VARIANT(type) == TYPE_MAIN_VARIANT(TREE_TYPE(expr)))
        return fold_convert(type, expr);

    switch (TREE_CODE(type)) {
        case VOID_TYPE:
        case BOOLEAN_TYPE:
            return fold_convert(type, expr);
        case INTEGER_TYPE:
            return fold(convert_to_integer(type, expr));
        case POINTER_TYPE:
            return fold(convert_to_pointer(type, expr));
        case REAL_TYPE:
            return fold(convert_to_real(type, expr));
        case COMPLEX_TYPE:
            return fold(convert_to_complex(type, expr));
        default:
            break;
    }

    gcc_unreachable();
}