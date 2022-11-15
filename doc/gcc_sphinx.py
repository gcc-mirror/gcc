# GCC Sphinx customization

__version__ = '1.0'


def setup(app):
    app.add_object_type('gcc-attr', 'gcc-attr', objname='GCC attribute',
                        indextemplate='pair: %s; attribute')
    app.add_object_type('fn-attr', 'fn-attr', objname='function attribute',
                        indextemplate='pair: %s; function attribute')
    app.add_object_type('var-attr', 'var-attr', objname='variable attribute',
                        indextemplate='pair: %s; variable attribute')
    app.add_object_type('type-attr', 'type-attr', objname='type attribute',
                        indextemplate='pair: %s; variable attribute')
    app.add_object_type('enum-attr', 'enum-attr', objname='Enumerator attribute',
                        indextemplate='pair: %s; enumerator attribute')
    app.add_object_type('label-attr', 'label-attr', objname='Label attribute',
                        indextemplate='pair: %s; label attribute')
    app.add_object_type('gcc-param', 'gcc-param', objname='GCC parameter',
                        indextemplate='pair: %s; parameter')

    targets = (('AArch64 ', 'aarch64'), ('AMD GCN ', 'amd-gcn'), ('ARC ', 'arc'), ('ARM ', 'arm'), ('AVR ', 'avr'),
               ('Blackfin ', 'blackfin'), ('BPF ', 'bpf'), ('C-SKY ', 'c-sky'),
               ('Epiphany ', 'epiphany'), ('H8/300 ', 'h8-300'), ('IA-64 ', 'ia-64'), ('LoongArch', 'loongarch'), ('M32C ', 'm32c'),
               ('M32R/D ', 'm32r-d'), ('m68k ', 'm68k'), ('MCORE ', 'mcore'), ('MeP ', 'mep'),
               ('MicroBlaze ', 'microblaze'), ('Microsoft Windows ', 'microsoft-windows'), ('MIPS ', 'mips'),
               ('MSP430 ', 'msp430'), ('NDS32 ', 'nds32'), ('Nios II ', 'nios-ii'), ('Nvidia PTX ', 'nvidia-ptx'),
               ('PowerPC ', 'powerpc'), ('RISC-V ', 'risc-v'), ('RL78 ', 'rl78'), ('RX ', 'rx'), ('S/390 ', 's-390'),
               ('SH ', 'sh'), ('Symbian OS ', 'symbian-os'), ('V850 ', 'v850'), ('Visium ', 'visium'), ('x86 ', 'x86'),
               ('Xstormy16 ', 'xstormy16'))

    for target_name, target in targets:
        app.add_object_type(f'{target}-fn-attr', f'{target}-fn-attr', objname=f'{target_name} function attribute',
                            indextemplate=f'pair: %s; {target_name} function attribute')
        app.add_object_type(f'{target}-var-attr', f'{target}-var-attr', objname=f'{target_name} variable attribute',
                            indextemplate=f'pair: %s; {target_name} variable attribute')
        app.add_object_type(f'{target}-type-attr', f'{target}-type-attr', objname=f'{target_name} type attribute',
                            indextemplate=f'pair: %s; {target_name} type attribute')

    return dict(
        version=__version__,
        parallel_read_safe=True,
        parallel_write_safe=True
    )
