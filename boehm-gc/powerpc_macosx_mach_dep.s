    .text
    
    .set   linkageArea,24
    .set   params,4
    .set   alignment,4

    .set   spaceToSave,linkageArea+params+alignment
    .set   spaceToSave8,spaceToSave+8

; Mark from machine registers that are saved by C compiler
    .globl  _GC_push_regs
_GC_push_regs:
    ; PROLOG
    mflr    r0          ; get return address
    stw     r0,8(r1)    ; save return address
    stwu    r1,-spaceToSave(r1)   ; skip over caller save area
    ;
    mr      r3,r2         ; mark from r2. Well I'm not really sure
                          ; that this is necessary or even the right
                          ; thing to do - at least it doesn't harm...
                          ; According to Apple's docs it points to
                          ; the direct data area, whatever that is...
    bl 	    _GC_push_one
    mr      r3,r13        ; mark from r13-r31
    bl 	    _GC_push_one
    mr      r3,r14
    bl 	    _GC_push_one
    mr      r3,r15
    bl 	    _GC_push_one
    mr      r3,r16
    bl 	    _GC_push_one
    mr      r3,r17
    bl 	    _GC_push_one
    mr      r3,r18
    bl 	    _GC_push_one
    mr      r3,r19
    bl 	    _GC_push_one
    mr      r3,r20
    bl 	    _GC_push_one
    mr      r3,r21
    bl 	    _GC_push_one
    mr      r3,r22
    bl 	    _GC_push_one
    mr      r3,r23
    bl 	    _GC_push_one
    mr      r3,r24
    bl 	    _GC_push_one
    mr      r3,r25
    bl 	    _GC_push_one
    mr      r3,r26
    bl 	    _GC_push_one
    mr      r3,r27
    bl 	    _GC_push_one
    mr      r3,r28
    bl 	    _GC_push_one
    mr      r3,r29
    bl 	    _GC_push_one
    mr      r3,r30
    bl 	    _GC_push_one
    mr      r3,r31
    bl 	    _GC_push_one
    ; EPILOG
    lwz     r0,spaceToSave8(r1)   ; get return address back
    mtlr    r0    ; reset link register
    addic   r1,r1,spaceToSave   ; restore stack pointer
    blr
