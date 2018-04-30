# define some latex elements to be used for PDF output

PAGE_BLANK = r'''
\makeatletter
\def\cleartooddpage{%%
   \cleardoublepage%%
}
\def\cleardoublepage{%%
\clearpage%%
   \if@twoside%%
      \ifodd\c@page%%
         %% nothing to do
      \else%%
         \hbox{}%%
         \thispagestyle{plain}%%
         \vspace*{\fill}%%
         \begin{center}%%
         \textbf{\em This page is intentionally left blank.}%%
         \end{center}%%
         \vspace{\fill}%%
         \newpage%%
         \if@twocolumn%%
            \hbox{}%%
            \newpage%%
         \fi%%
      \fi%%
   \fi%%
}
\makeatother
'''

TOC_DEPTH = r'''
\pagenumbering{arabic}
\setcounter{tocdepth}{3}
'''

TOC_CMD = r'''
\makeatletter
\def\tableofcontents{%%
    \pagestyle{plain}%%
    \chapter*{\contentsname}%%
    \@mkboth{\MakeUppercase{\contentsname}}%%
            {\MakeUppercase{\contentsname}}%%
    \@starttoc{toc}%%
}
\makeatother
'''

with open('copyright.tex', 'r') as fd:
    copyright = fd.read()

TOC = r'''
\cleardoublepage
%s
\cleardoublepage
\tableofcontents
\cleardoublepage\pagestyle{plain}
''' % copyright

LATEX_HYPHEN = r'''
\hyphenpenalty=5000
\tolerance=1000
'''


def doc_settings(full_document_name, version):
    return '\n'.join([
        r'\newcommand*{\GNATFullDocumentName}[0]{' + full_document_name + r'}',
        r'\newcommand*{\GNATVersion}[0]{' + version + r'}'])
