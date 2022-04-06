module imports.test9276expr;

import imports.test9276parser;
import imports.test9276util;

class Node
{
    mixin DownCastMethods!Declaration;

}

class Expression : Node
{
}
