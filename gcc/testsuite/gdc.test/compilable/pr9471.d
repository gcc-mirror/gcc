// PERMUTE_ARGS:
// EXTRA_FILES: imports/pr9471a.d imports/pr9471b.d imports/pr9471c.d imports/pr9471d.d
import imports.pr9471a;
import imports.pr9471b;

static assert (__traits(getVirtualIndex, ClassDeclaration.isBaseOf) == 7);
