// REQUIRED_ARGS: -o- -w

int bug17807(){
    int y=0;
    Lswitch: switch(2){
        { case 0: break; }
        enum x=0;
        struct S{ enum x=0; }
        int foo(){
            return 0;
        }
        default: y=x+S.x+foo();
        static foreach(i;1..5)
            case i: break Lswitch;
    }
    return y;
}
