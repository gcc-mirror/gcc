/******************************************************************
*******************************************************************
***                                                             ***
***     Linker script for xstormy16-elf-gcc                     ***
***           For SID   RAM=0x7E00                              ***
***                     ROM=0x78000                             ***
***                                                             ***
*******************************************************************
******************************************************************/

OUTPUT_FORMAT("elf32-xstormy16", "elf32-xstormy16", "elf32-xstormy16")
OUTPUT_ARCH(xstormy16)
ENTRY(_start)
SEARCH_DIR( . )
GROUP(-lc -lsim -lgcc)
PROVIDE( __target_package = 0xff);

__malloc_start = 0x7E00;

MEMORY
{
	RAM (w)    : ORIGIN = 0x00000, LENGTH = 0x07E00
	CHIP (r)   : ORIGIN = 0x07f00, LENGTH = 0x00100
	ROM (!w)   : ORIGIN = 0x08000, LENGTH = 0x78000
}

SECTIONS
{
	/* Zero initialized data with the below100 attribute.  */
	.bss_below100 : {
		SHORT(0)
		__bss_start = .;
		*(.bss_below100)
		*(.bss_below100.*)
		. = ALIGN(2);
	} > RAM
	
	/* Non-zero initialized data with the below100 attribute.  */
	.data_below100 : AT ( __rdata ) {
		__data_start = . ;
	        *(.data_0)
		*(.data_below100)
		*(.data_below100.*)
		. = ALIGN(2);
	} > RAM = 0
	
        /* Normal non-zero initialized data.  */
       .data : AT ( __rdata + SIZEOF(.data_below100) ) {
		*(.data)
		*(.data.*)
		*(.gnu.linkonce.d.*)
		SORT(CONSTRUCTORS)
		. = ALIGN(2);
		__data_end = . ;
	} > RAM =0
	
        /* Normal zero initialized data.  */
	.bss : AT (LOADADDR(.data) + SIZEOF(.data)) {
        	*(.dynbss)
		*(.bss)
		*(.bss.*)
		*(.gnu.linkonce.b.*)
		*(COMMON)
		. = ALIGN(2);
		__bss_end = .;
	} > RAM
	
        /* The top of stack.  */
	__stack = .;

        /* Target chip info.  */
	.chip_info : {
		KEEP(*(.chip_info))
	} > CHIP =0
	
        /* Reset and interrupt vectors at 8000.  */
	.int_vec : {
	        KEEP(*(.int_vec))
	} > ROM =0
	
        /* Read only data.  */
	.rodata : {
        	*(.rodata)
		*(.rodata.*)
		*(.gnu.linkonce.r.*)
	} > ROM =0
	
        /* C++ Construcrtors and destructors.  */
	.ctors : {
		KEEP (*crtbegin.o(.ctors))
		KEEP (*(EXCLUDE_FILE (*crtend.o ) .ctors))
		KEEP (*(SORT(.ctors.*)))
		KEEP (*(.ctors))
	} > ROM =0
	
	.dtors : {
        	KEEP (*crtbegin.o(.dtors))
		KEEP (*(EXCLUDE_FILE (*crtend.o ) .dtors))
		KEEP (*(SORT(.dtors.*)))
		KEEP (*(.dtors))
	} > ROM =0

        /* Pointer lookup table.. */
	.plt : {
	        *(.plt)
	} > ROM =0

        /* Other information.  */
	.jcr : {
	        KEEP (*(.jcr))
	} > ROM =0
	
	.eh_frame : {
	        KEEP (*(.eh_frame))
	} > ROM =0

	.gcc_except_table : {
	        KEEP (*(.gcc_except_table))
	} > ROM =0

        /* Initialization values for data.  */
	.data_init (NOLOAD) : {
		__rdata = .;
		. += SIZEOF(.data_below100);
		. += SIZEOF(.data);
		. += SIZEOF(.bss);
	} > ROM

        /* Executable code.  */
	.text : {
		*(.text)
		*(.text.*)
		*(.stub)
		*(.gnu.warning)
		*(.gnu.linkonce.t.*)
	} > ROM =0

        /* Startup/finish code.  */
	.init : {
        	KEEP (*crti.o(.init))
		KEEP (*(EXCLUDE_FILE (*crtn.o ) .init))
		KEEP (*(SORT(.init.*)))
		KEEP (*(.init))
	} > ROM =0
	
	.fini : {
        	KEEP (*crti.o(.fini))
		KEEP (*(EXCLUDE_FILE (*crtn.o ) .fini))
		KEEP (*(SORT(.fini.*)))
		KEEP (*(.fini))
	} > ROM =0

        /* Stab debugging sections.  */
	.stab          0 : { *(.stab) }
	.stabstr       0 : { *(.stabstr) }
	.stab.excl     0 : { *(.stab.excl) }
	.stab.exclstr  0 : { *(.stab.exclstr) }
	.stab.index    0 : { *(.stab.index) }
	.stab.indexstr 0 : { *(.stab.indexstr) }
	.comment       0 : { *(.comment) }

        /* DWARF debug sections.  */	
	/* Symbols in the DWARF debugging sections are relative to
	   the beginning of the section so we begin them at 0.  */

   	/* DWARF 1.  */
	.debug 0 : { *(.debug) }
	.line 0 : { *(.line) }

	/* GNU DWARF 1 extensions.  */
	.debug_srcinfo 0 : { *(.debug_srcinfo) }
	.debug_sfnames 0 : { *(.debug_sfnames) }

	/* DWARF 1.1 and DWARF 2.  */
	.debug_aranges  0 : { *(.debug_aranges) }
	.debug_pubnames 0 : { *(.debug_pubnames) }

	/* DWARF 2.  */
	.debug_info     0 : { *(.debug_info) *(.gnu.linkonce.wi.*) }
	.debug_abbrev   0 : { *(.debug_abbrev) }
	.debug_line     0 : { *(.debug_line) }
	.debug_frame    0 : { *(.debug_frame) }
	.debug_str      0 : { *(.debug_str) }
	.debug_loc      0 : { *(.debug_loc) }
	.debug_macinfo  0 : { *(.debug_macinfo) }

        /* SGI/MIPS DWARF 2 extensions.  */
	.debug_weaknames 0 : { *(.debug_weaknames) }
	.debug_funcnames 0 : { *(.debug_funcnames) }
	.debug_typenames 0 : { *(.debug_typenames) }
	.debug_varnames  0 : { *(.debug_varnames) }
}
