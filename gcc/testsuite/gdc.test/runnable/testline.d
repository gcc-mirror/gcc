// PERMUTE_ARGS:

// $HeadURL$
// $Date$
// $Author$

module dstress.run.line_token_03;

import core.exception;

int main(){
        try{
                #line 1 ""
                assert(0);
        }catch(AssertError o){
                checkFileSpec(o);
                return 0;
        }

        assert(0);
}

/*
 * @WARNING@: this code depends on the phobos implementation.
 * char[]s returned by wrong assertions have to look like:
 *       "blah blah "filename" blah blah"
 */
void checkFileSpec(Object o){
        string str=o.toString();

        int start;
        for(start=0; start<str.length; start++){
                if(str[start]=='('){
                        break;
                }
        }

        assert(str[start .. start+3]=="(1)");
}
