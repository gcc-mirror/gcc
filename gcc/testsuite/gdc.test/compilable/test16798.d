/*
REQUIRED_ARGS: -mv=its.a.dessert.topping=imports/imp16798.d -mv=its.a.floorwax=imports/
EXTRA_FILES: imports/imp16798.d imports/wax16798.d
PERMUTE_ARGS:
TEST_OUTPUT:
---
it's a floor wax
it's a dessert topping
---
*/

import its.a.floorwax.wax16798;
import its.a.dessert.topping;
