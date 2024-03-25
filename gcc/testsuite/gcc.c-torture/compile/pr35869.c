struct texture_stage_op
{
    unsigned int carg1, carg2, carg0;
    unsigned int aarg1, aarg2, aarg0;
    unsigned int dst;
};

static const char *debug_register(unsigned int reg) {
    switch(reg) {
        case 0x8921: return "GL_REG_0_ATI";
        case 0x8923: return "GL_REG_2_ATI";
        case 0x0: return "GL_ZERO";
        case 0x1: return "GL_ONE";
        default: return "Unknown register\n";
    }
}

static unsigned int find_tmpreg(struct texture_stage_op op[8]) {
    int i;
    int tex_used[8];

    for(i = 0; i < 8; i++) {
        if(op[i].carg1 == 0x00000002 ) {
            tex_used[i] = 1;
        }
    }

    for(i = 1; i < 6; i++) {
        if(!tex_used[i]) {
                return 0x8921 + i;
        }
    }
    return 0;
}

extern void f(const char*);

void g() {
    struct texture_stage_op op[8];
    unsigned int tmparg = find_tmpreg(op);
    unsigned int dstreg;

    if(tmparg == 0x0) return;
    dstreg = tmparg;
    f(debug_register(dstreg));
    return;
}
