
        name    gc_watcom

.386p

        extrn   _edata              : byte  ; end of DATA (start of BSS)
        extrn   _end                : byte  ; end of BSS (start of STACK)
        extrn   __nullarea          : word

        extrn   "C",_STACKLOW          : dword
        extrn   "C",_STACKTOP          : dword


DGROUP  group   _DATA

_DATA   segment dword public 'DATA'
_DATA   ends

_TEXT   segment para public use32 'CODE'
        assume  cs:_TEXT, ds:DGROUP, ss:DGROUP

        public  Get_DATASTART
        align   4
Get_DATASTART   proc near

        mov     eax,offset DGROUP:__nullarea
        ret

Get_DATASTART   endp

        public  Get_DATAEND
        align   4
Get_DATAEND     proc near

        mov     eax,offset DGROUP:_end
        ret

Get_DATAEND     endp

        public  Get_STACKBOTTOM
        align   4
Get_STACKBOTTOM proc near

        mov     eax,_STACKTOP
        ret

Get_STACKBOTTOM endp

_TEXT   ends

        end
