"""Alternate Ada and Project Files parsers for Sphinx/Rest"""

import re
from pygments.lexer import RegexLexer, bygroups
from pygments.token import Text, Comment, Operator, Keyword, Name, String, \
    Number, Punctuation


def get_lexer_tokens(tag_highlighting=False, project_support=False):
    """Return the tokens needed for RegexLexer

    :param tag_highlighting: if True we support tag highlighting. See
        AdaLexerWithTags documentation
    :type tag_highlighting: bool
    :param project_support: if True support additional keywors associated
        with project files.
    :type project_support:  bool

    :return: a dictionary following the structure required by RegexLexer
    :rtype: dict
    """
    if project_support:
        project_pattern = r'project\s+|'
        project_pattern2 = r'project|'
    else:
        project_pattern = r''
        project_pattern2 = r''

    result = {
        'root': [
            # Comments
            (r'--.*$', Comment),
            # Character literal
            (r"'.'", String.Char),
            # Strings
            (r'"[^"]*"', String),
            # Numeric
            # Based literal
            (r'[0-9][0-9_]*#[0-9a-f][0-9a-f_]*#(E[\+-]?[0-9][0-9_]*)?',
             Number.Integer),
            (r'[0-9][0-9_]*#[0-9a-f][0-9a-f_]*'
             r'\.[0-9a-f][0-9a-f_]*#(E[\+-]?[0-9][0-9_]*)?', Number.Float),
            # Decimal literal
            (r'[0-9][0-9_]*\.[0-9][0-9_](E[\+-]?[0-9][0-9_]*)?', Number.Float),
            (r'[0-9][0-9_]*(E[\+-]?[0-9][0-9_]*)?', Number.Integer),
            # Match use and with statements
            # The first part of the pattern is be sure we don't match
            # for/use constructs.
            (r'(\n\s*|;\s*)(with|use)(\s+[\w\.]+)',
             bygroups(Punctuation, Keyword.Reserved, Name.Namespace)),
            # Match procedure, package and function declarations
            (r'end\s+(if|loop|record)', Keyword),
            (r'(package(?:\s+body)?\s+|' + project_pattern +
             r'function\s+|end\s+|procedure\s+)([\w\.]+)',
             bygroups(Keyword, Name.Function)),
            # Ada 2012 standard attributes, GNAT specific ones and
            # Spark 2014 ones ('Update and 'Loop_Entry)
            # (reversed order to avoid having for
            #  example Max before Max_Alignment_For_Allocation).
            (r'\'(Write|Width|Wide_Width|Wide_Wide_Width|Wide_Wide_Value|'
             r'Wide_Wide_Image|Wide_Value|Wide_Image|Word_Size|Wchar_T_Size|'
             r'Version|Value_Size|Value|Valid_Scalars|VADS_Size|Valid|Val|'
             r'Update|Unrestricted_Access|Universal_Literal_String|'
             r'Unconstrained_Array|Unchecked_Access|Unbiased_Rounding|'
             r'Truncation|Type_Class|To_Address|Tick|Terminated|'
             r'Target_Name|Tag|System_Allocator_Alignment|Succ|Stub_Type|'
             r'Stream_Size|Storage_Unit|Storage_Size|Storage_Pool|Small|Size|'
             r'Simple_Storage_Pool|Signed_Zeros|Scaling|Scale|'
             r'Scalar_Storage_Order|Safe_Last|Safe_Large|Safe_First|'
             r'Safe_Emax|Rounding|Round|Result|Remainder|Ref|Read|'
             r'Range_Length|Range|Priority|Pred|'
             r'Position|Pos|Pool_Address|Passed_By_Reference|Partition_Id|'
             r'Overlaps_Storage|Output|Old|Object_Size|Null_Parameter|Modulus|'
             r'Model_Small|Model_Mantissa|Model_Epsilon|Model_Emin|Model|Mod|'
             r'Min|Mechanism_Code|Maximum_Alignment|'
             r'Max_Size_In_Storage_Elements|Max_Priority|'
             r'Max_Interrupt_Priority|Max_Alignment_For_Allocation|'
             r'Max|Mantissa|Machine_Size|Machine_Rounds|Machine_Rounding|'
             r'Machine_Radix|Machine_Overflows|Machine_Mantissa|Machine_Emin|'
             r'Machine_Emax|Machine|Loop_Entry|Length|Length|Leading_Part|'
             r'Last_Valid|Last_Bit|Last|Large|Invalid_Value|Integer_Value|'
             r'Input|Image|Img|Identity|Has_Same_Storage|Has_Discriminants|'
             r'Has_Access_Values|Fraction|Fore|Floor|Fixed_Value|First_Valid|'
             r'First_Bit|First|External_Tag|Exponent|Epsilon|Enum_Val|'
             r'Enum_Rep|Enabled|Emax|Elaborated|Elab_Subp_Body|Elab_Spec|'
             r'Elab_Body|Descriptor_Size|Digits|Denorm|Delta|Definite|'
             r'Default_Bit_Order|Count|Copy_Sign|Constrained|'
             r'Compose|Component_Size|Compiler_Version|Code_Address|Class|'
             r'Ceiling|Caller|Callable|Body_Version|Bit_Order|Bit_Position|'
             r'Bit|Base|Asm_Output|Asm_Input|Alignment|Aft|Adjacent|'
             r'Address_Size|Address|Access|Abort_Signal|AST_Entry)',
             Name.Attribute),
            # All Ada2012 reserved words
            (r'(abort|abstract|abs|accept|access|aliased|all|and|array|at|'
             r'begin|body|case|constant|declare|delay|delta|digits|do|'
             r'else|elsif|end|entry|exception|exit|for|function|generic|goto|'
             r'if|interface|in|is|limited|loop|mod|new|not|null|'
             r'of|or|others|out|overriding|' + project_pattern2 +
             r'package|pragma|private|procedure|protected|'
             r'raise|range|record|rem|renames|requeue|return|reverse|'
             r'select|separate|some|subtype|synchronized|'
             r'tagged|task|terminate|then|type|until|use|when|while|with|xor'
             r')([\s;,])',
             bygroups(Keyword.Reserved, Punctuation)),
            # Two characters operators
            (r'=>|\.\.|\*\*|:=|/=|>=|<=|<<|>>|<>', Operator),
            # One character operators
            (r'&|\'|\(|\)|\*|\+|-|\.|/|:|<|=|>|\|', Operator),
            (r',|;', Punctuation),
            # Spaces
            (r'\s+', Text),
            # Builtin values
            (r'False|True', Keyword.Constant),
            # Identifiers
            (r'[\w\.]+', Name)], }

    # Insert tag highlighting before identifiers
    if tag_highlighting:
        result['root'].insert(-1, (r'\[[\w ]*\]', Name.Tag))

    return result


class AdaLexer(RegexLexer):
    """Alternate Pygments lexer for Ada source code and project files

    The default pygments lexer always fails causing disabling of syntax
    highlighting in Sphinx. This lexer is simpler but safer.

    In order to use this lexer in your Sphinx project add the following
    code at the end of your conf.py

    .. code-block:: python

        import gnatpython.ada_pygments

        def setup(app):
            app.add_lexer('ada', gnatpython.ada_pygments.AdaLexer())

    """
    name = 'Ada'
    aliases = ['ada', 'ada83', 'ada95', 'ada2005', 'ada2012']
    filenames = ['*.adb', '*.ads', '*.ada']
    mimetypes = ['text/x-ada']

    flags = re.MULTILINE | re.I  # Ignore case

    tokens = get_lexer_tokens()


class TaggedAdaLexer(AdaLexer):
    """Alternate Pygments lexer for Ada source code with tags

    A tag is a string of the form::

      [MY STRING]

    Only alphanumerical characters and spaces are considered inside the
    brackets.
    """

    name = 'TaggedAda'
    aliases = ['tagged_ada']
    tokens = get_lexer_tokens(True)


class GNATProjectLexer(RegexLexer):
    """Pygment lexer for project files

    This is the same as the AdaLexer but with support of ``project``
    keyword.
    """
    name = 'GPR'
    aliases = ['gpr']
    filenames = ['*.gpr']
    mimetypes = ['text/x-gpr']

    flags = re.MULTILINE | re.I  # Ignore case

    tokens = get_lexer_tokens(project_support=True)
